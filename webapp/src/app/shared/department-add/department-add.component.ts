import { Component, model, output } from '@angular/core';
import { Katedra } from '../../core';
import { InputComponent } from '../input/input.component';

@Component({
  selector: 'app-department-add',
  standalone: true,
  imports: [InputComponent],
  templateUrl: './department-add.component.html',
  styleUrl: './department-add.component.scss',
})
export class DepartmentAddComponent {
  data = model.required<Katedra>();
  query = output<Katedra>();

  edit(field: string, value: unknown): void {
    this.data.update((d) => {
      d[field] = value;
      return d;
    });
    this.query.emit(this.data());
  }
}
