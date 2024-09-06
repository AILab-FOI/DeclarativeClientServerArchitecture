import { Component, input, model, output } from '@angular/core';
import { Sekcija } from '../../../assets/pkg/client';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-section-add',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './section-add.component.html',
  styleUrl: './section-add.component.scss',
})
export class SectionAddComponent {
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
