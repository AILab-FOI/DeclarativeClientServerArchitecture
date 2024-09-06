import { Component, model, output, signal } from '@angular/core';
import { Sekcija } from '../../core';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-section-edit',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './section-edit.component.html',
  styleUrl: './section-edit.component.scss',
})
export class SectionEditComponent {
  data = model.required<Sekcija>();
  query = output<Sekcija>();

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
}
