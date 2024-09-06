import {
  Component,
  computed,
  model,
  output,
  signal,
  TemplateRef,
  viewChild,
} from '@angular/core';
import { InputComponent } from '../input/input.component';
import { Sadrzaj } from '../../core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-content-add',
  standalone: true,
  imports: [InputComponent, CommonModule],
  templateUrl: './content-add.component.html',
  styleUrl: './content-add.component.scss',
})
export class ContentAddComponent {
  data = model.required<Sadrzaj>();

  query = output<Sadrzaj>();

  selected = signal<string>('dokument');

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  editValue(field: string, value: unknown): void {
    this.data.update((d) => {
      d.vrijednost[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }

  editType(value: any): void {
    this.data.update((d) => {
      d.tip = value;
      return d;
    });
    this.selected.set(value);
    this.query.emit(this.data());
  }
}
